#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class print_crdb_memo2 : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "print_crdb_memo2.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = PrmComp;
            if (aUsers == "external")
            {

            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {
            OutPutFile.Visible = false;
            HyperLink1.Visible = false;

            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;
            }


            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'print_crdb_memo2.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {

                    becustTextBox.Text = dr["field1"].ToString();
                    endcustTextBox.Text = dr["field2"].ToString();
                    bedateTextBox.Text = dr["field3"].ToString();
                    enddateTextBox.Text = dr["field4"].ToString();
                    bememoTextBox.Text = dr["field5"].ToString();
                    endmemoTextBox.Text = dr["field6"].ToString();




                    if (dr["chk_field1"].ToString() == "yes")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;
                    if (dr["chk_field2"].ToString() == "yes")
                        CheckBox2.Checked = true;
                    else
                        CheckBox2.Checked = false;
                    if (dr["chk_field3"].ToString() == "yes")
                        CheckBox3.Checked = true;
                    else
                        CheckBox3.Checked = false;


                }

                conn.Close();
            }
            catch
            {
                conn.Close();
            }
            finally
            {
                conn.Close();
            }


            try
            {

                account val = new account();
                DataSet dsget = new DataSet();
                dsget = val.PrintCreditDebitMemo(UserLogin.UserName, "getvalue", "", 0, "", "", 0, "", "", "", "", "");
                if (Convert.ToString(dsget.Tables[0].Rows[0][1]) == "yes" || Convert.ToString(dsget.Tables[0].Rows[0][1]) == "YES")
                {
                    CheckBox3.Visible = false;
                }
                else
                {
                    CheckBox3.Visible = true;
                }


                if (endcustTextBox.Text == "")
                    endcustTextBox.Text = "zzzzzzzz";
                if (endmemoTextBox.Text == "")
                    endmemoTextBox.Text = "999999999";

            }
            catch { }

        }
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void submitbutton_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (CheckBox1.Checked)
            HiddenField1.Value = "yes";
        else
            HiddenField1.Value = "no";
        if (CheckBox2.Checked)
            HiddenField2.Value = "yes";
        else
            HiddenField2.Value = "no";
        if (CheckBox3.Checked)
            HiddenField3.Value = "yes";
        else
            HiddenField3.Value = "no";

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "prcd";
        ObjectDataSource1.SelectParameters["prmbegcust"].DefaultValue = becustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendcust"].DefaultValue = endcustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegmemo"].DefaultValue = bememoTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendmemo"].DefaultValue = endmemoTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegdt"].DefaultValue = bedateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmenddt"].DefaultValue = enddateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmprtmo"].DefaultValue = Convert.ToString(CheckBox1.Checked);
        ObjectDataSource1.SelectParameters["prmpstmo"].DefaultValue = Convert.ToString(CheckBox2.Checked);
        ObjectDataSource1.SelectParameters["prmexprt"].DefaultValue = Convert.ToString(CheckBox3.Checked);



        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'print_crdb_memo2.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, chk_field1, chk_field2, chk_field3) values ('" + UserLogin.UserName + "','print_crdb_memo2.aspx' , '" + becustTextBox.Text.Trim() + "', '" + endcustTextBox.Text.Trim() + "', '" + bedateTextBox.Text.Trim() + "', '" + enddateTextBox.Text.Trim() + "', '" + bememoTextBox.Text.Trim() + "', '" + endmemoTextBox.Text.Trim() + "', '" + HiddenField1.Value + "', '" + HiddenField2.Value + "', '" + HiddenField3.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + becustTextBox.Text.Trim() + "', field2 = '" + endcustTextBox.Text.Trim() + "', field3 = '" + bedateTextBox.Text.Trim() + "', field4 = '" + enddateTextBox.Text.Trim() + "',field5 = '" + bememoTextBox.Text.Trim() + "',field6 = '" + endmemoTextBox.Text.Trim() + "', chk_field1 = '" + HiddenField1.Value + "', chk_field2 = '" + HiddenField2.Value + "', chk_field3 = '" + HiddenField3.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'print_crdb_memo2.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }
            conn.Close();
        }
        catch (Exception ex)
        {
            Label1.Text = "Error :" + ex.Message + "<p>";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }


        try
        {

            OutPutFile.Visible = true;
            HyperLink1.Visible = true;
            Label vpath = (Label)FormView1.FindControl("prcdLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;

            if (vpath.Text == "")
            {
                OutPutFile.Text = "No Pdf Exists";
                Response.Write("<script>window.location.href='print_crdb_memo2.aspx'</script>");
            }
        }
        catch { }

    }

}
