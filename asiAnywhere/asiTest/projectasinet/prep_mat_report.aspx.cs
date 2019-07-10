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

public partial class prep_mat : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "prep_mat_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;

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

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'prep_mat_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    BegSalrepTextBox.Text       = dr["field1"].ToString();
                    EndSalrepTextBox.Text       = dr["field2"].ToString();
                    BegCustTextBox.Text         = dr["field3"].ToString();
                    EndCustTextBox.Text         = dr["field4"].ToString();
                    BegLastModdateTextBox.Text  = dr["field5"].ToString();
                    EndLastModdateTextBox.Text  = dr["field6"].ToString();
                    BegLastOrddateTextBox.Text  = dr["field7"].ToString();
                    EndLastOrddateTextBox.Text  = dr["field8"].ToString();
                    BegPrepTextBox.Text         = dr["field9"].ToString();
                    EndPrepTextBox.Text         = dr["field10"].ToString();

                    if (dr["chk_field1"].ToString() == "Yes")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;
                    if (dr["chk_field2"].ToString() == "Yes")
                        CheckBox2.Checked = true;
                    else
                        CheckBox2.Checked = false;
                    if (dr["chk_field3"].ToString() == "Yes")
                        CheckBox3.Checked = true;
                    else
                        CheckBox3.Checked = false;
                    if (dr["chk_field4"].ToString() == "Yes")
                        CheckBox4.Checked = true;
                    else
                        CheckBox4.Checked = false;
                    if (dr["chk_field5"].ToString() == "Yes")
                        CheckBox5.Checked = true;
                    else
                        CheckBox5.Checked = false;         

                    if (dr["rd_field1"].ToString() == "Prep Code")
                        RadioButtonList1.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "SalesRep/Cust")
                        RadioButtonList1.SelectedIndex = 1;
                    if (dr["rd_field1"].ToString() == "Order Date")
                        RadioButtonList1.SelectedIndex = 2;
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
                if (EndCustTextBox.Text == "")
                    EndCustTextBox.Text = "zzzzzzzz";
                if (EndLastModdateTextBox.Text == "")
                    EndLastModdateTextBox.Text = "12/31/9999";
                if (EndLastOrddateTextBox.Text == "")
                    EndLastOrddateTextBox.Text = "12/31/9999";
                if (EndSalrepTextBox.Text == "")
                    EndSalrepTextBox.Text = "zzz";
                if (EndPrepTextBox.Text == "")
                    EndPrepTextBox.Text = "zzzzzzzzzzzzzzz";

                RadioButtonList_out.SelectedIndex = 0;
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
            HiddenField1.Value = "Yes";       
        else     
            HiddenField1.Value = "No";
        if (CheckBox2.Checked)
            HiddenField2.Value = "Yes";
        else
            HiddenField2.Value = "No";
        if (CheckBox3.Checked)
            HiddenField3.Value = "Yes";
        else
            HiddenField3.Value = "No";
        if (CheckBox4.Checked)
            HiddenField4.Value = "Yes";
        else
            HiddenField4.Value = "No";
        if (CheckBox5.Checked)
            HiddenField5.Value = "Yes";
        else
            HiddenField5.Value = "No";

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "PrepMatRep";
        ObjectDataSource1.SelectParameters["prmBegCustomer"].DefaultValue = BegCustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCustomer"].DefaultValue = EndCustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegLastModDate"].DefaultValue = BegLastModdateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndLastModDate"].DefaultValue = EndLastModdateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegLastOrdDate"].DefaultValue = BegLastOrddateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndLastOrdDate"].DefaultValue = EndLastOrddateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegSalrep"].DefaultValue = BegSalrepTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndSalrep"].DefaultValue = EndSalrepTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegPrepCode"].DefaultValue = BegPrepTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndPrepCode"].DefaultValue = EndPrepTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDie"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmPlate"].DefaultValue = HiddenField2.Value;
        ObjectDataSource1.SelectParameters["prmFolding"].DefaultValue = HiddenField3.Value;
        ObjectDataSource1.SelectParameters["prmCorrugated"].DefaultValue = HiddenField4.Value;
        ObjectDataSource1.SelectParameters["prmSortBy"].DefaultValue = RadioButtonList1.SelectedValue;
        ObjectDataSource1.SelectParameters["prmprintDscr"].DefaultValue = HiddenField5.Value;
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'prep_mat_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, chk_field1, chk_field2, chk_field3, chk_field4, chk_field5, rd_field1) values ('" + UserLogin.UserName + "','prep_mat_report.aspx' , '" + BegSalrepTextBox.Text.Trim() + "', '" + EndSalrepTextBox.Text.Trim() + "', '" + BegCustTextBox.Text.Trim() + "', '" + EndCustTextBox.Text.Trim() + "', '" + BegLastModdateTextBox.Text.Trim() + "', '" + EndLastModdateTextBox.Text.Trim() + "', '" + BegLastOrddateTextBox.Text.Trim() + "', '" + EndLastOrddateTextBox.Text.Trim() + "', '" + BegPrepTextBox.Text.Trim() + "', '" + EndPrepTextBox.Text.Trim() + "', '" + HiddenField1.Value + "', '" + HiddenField2.Value + "', '" + HiddenField3.Value + "', '" + HiddenField4.Value + "', '" + HiddenField5.Value + "', '" + RadioButtonList1.SelectedValue + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + BegSalrepTextBox.Text.Trim() + "', field2 = '" + EndSalrepTextBox.Text.Trim() + "', field3 = '" + BegCustTextBox.Text.Trim() + "', field4 = '" + EndCustTextBox.Text.Trim() + "', field5 = '" + BegLastModdateTextBox.Text.Trim() + "', field6 = '" + EndLastModdateTextBox.Text.Trim() + "', field7 = '" + BegLastOrddateTextBox.Text.Trim() + "', field8 = '" + EndLastOrddateTextBox.Text.Trim() + "', field9 = '" + BegPrepTextBox.Text.Trim() + "', field10 = '" + EndPrepTextBox.Text.Trim() + "', chk_field1 = '" + HiddenField1.Value + "', chk_field2 = '" + HiddenField2.Value + "', chk_field3 = '" + HiddenField3.Value + "', chk_field4 = '" + HiddenField4.Value + "', chk_field5 = '" + HiddenField5.Value + "', rd_field1 = '" + RadioButtonList1.SelectedValue + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'prep_mat_report.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("pMatFileLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                //Response.Write("<script>window.location.href='prep_mat_report.aspx'</script>");
            }
        }
        catch { }               
    }

   }
