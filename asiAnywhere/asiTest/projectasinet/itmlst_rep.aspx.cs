using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Data.SqlClient;

public partial class itmlst_rep: System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "itmlst_rep.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Session["ytd_login"] = PrmComp;
            labelcompany.Text = PrmComp;
            if (aUsers == "external")
            {
                //txt_customer.Visible = false;
                //CustomerLook.Visible = false;
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        if (!Page.IsPostBack)
        {
            ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["ytd_login"]);

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'itmlst_rep.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    txt_beginware.Text = dr["field1"].ToString();
                    txt_endware.Text = dr["field2"].ToString();
                    txt_begincust.Text = dr["field3"].ToString();
                    txt_endcust.Text = dr["field4"].ToString();
                    txt_beginitem.Text = dr["field5"].ToString();
                    txt_enditem.Text = dr["field6"].ToString();
                    txt_begcat.Text = dr["field7"].ToString();
                    txt_endcat.Text = dr["field8"].ToString();


                    if (dr["chk_field1"].ToString() == "True")
                        Zero_CheckBox.Checked = true;
                    else
                        Zero_CheckBox.Checked = false;
                    if (dr["chk_field2"].ToString() == "True")
                        Ware_CheckBox.Checked = true;
                    else
                        Ware_CheckBox.Checked = false;
                    if (dr["chk_field3"].ToString() == "True")
                        Cat_CheckBox.Checked = true;
                    else
                        Cat_CheckBox.Checked = false;

                    if (dr["rd_field1"].ToString() == "Customer#")
                        RadioButtonList1.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "Item#")
                        RadioButtonList1.SelectedIndex = 1;


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
                Label begin = (Label)FormView2.FindControl("CustLabel");
                if (txt_begincust.Text == "")
                {
                    txt_begincust.Text = begin.Text;
                    txt_endcust.Text = begin.Text;
                }
            }
            catch { }
            if (Session["User"] != null)
            {
                //UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;
                Session["ytd_user_login"] = lblUser.Text;
            }
            OutputLabel.Visible = false;
            HyperLink1.Visible = false;
            if(txt_enditem.Text == "")
            txt_enditem.Text = "zzzzzzzzzzzzzzz";
            if(txt_endcat.Text == "")
                txt_endcat.Text = "zzzzzzzz";
            if (txt_endware.Text == "")
            txt_endware.Text = "zzz";
            RadioButtonList8.SelectedIndex = 0;

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
    protected void SubmitButton_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        if (RadioButtonList8.SelectedIndex == 0)
            HiddenField_out.Value = "No";
        if (RadioButtonList8.SelectedIndex == 1)
            HiddenField_out.Value = "Yes";

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Finished";
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = HiddenField_out.Value;
        ObjectDataSource1.SelectParameters["prmBeWare"].DefaultValue = txt_beginware.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndWare"].DefaultValue = txt_endware.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeCust"].DefaultValue = txt_begincust.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = txt_endcust.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeItem"].DefaultValue = txt_beginitem.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = txt_enditem.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeCat"].DefaultValue = txt_begcat.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCat"].DefaultValue = txt_endcat.Text.Trim();
        ObjectDataSource1.SelectParameters["prmZero"].DefaultValue = Convert.ToString(Zero_CheckBox.Checked);

        ObjectDataSource1.SelectParameters["prmCustWhse"].DefaultValue = Convert.ToString(Ware_CheckBox.Checked);
        ObjectDataSource1.SelectParameters["prmSort"].DefaultValue = Convert.ToString(RadioButtonList1.SelectedValue);
        ObjectDataSource1.SelectParameters["prmProdCat"].DefaultValue = Convert.ToString(Cat_CheckBox.Checked);


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'itmlst_rep.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8, chk_field1,chk_field2,chk_field3,rd_field1) values ('" + UserLogin.UserName + "','itmlst_rep.aspx' , '" + txt_beginware.Text.Trim() + "', '" + txt_endware.Text.Trim() + "', '" + txt_begincust.Text.Trim() + "', '" + txt_endcust.Text.Trim() + "', '" + txt_beginitem.Text.Trim() + "', '" + txt_enditem.Text.Trim() + "', '" + txt_begcat.Text.Trim() + "', '" + txt_endcat.Text.Trim() + "', '" + Convert.ToString(Zero_CheckBox.Checked) + "','" + Convert.ToString(Ware_CheckBox.Checked) + "','" + Convert.ToString(Cat_CheckBox.Checked) + "','"+ Convert.ToString(RadioButtonList1.SelectedValue)+"')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + txt_beginware.Text.Trim() + "', field2 = '" + txt_endware.Text.Trim() + "', field3 = '" + txt_begincust.Text.Trim() + "', field4 = '" + txt_endcust.Text.Trim() + "', field5 = '" + txt_beginitem.Text.Trim() + "', field6 = '" + txt_enditem.Text.Trim() + "',field7 = '" + txt_begcat.Text.Trim() + "',field8 = '" + txt_endcat.Text.Trim() + "', chk_field1 = '" + Convert.ToString(Zero_CheckBox.Checked) + "',chk_field2 = '" + Convert.ToString(Ware_CheckBox.Checked) + "',chk_field3 = '" + Convert.ToString(Cat_CheckBox.Checked) + "',rd_field1 = '" + Convert.ToString(RadioButtonList1.SelectedValue) + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'itmlst_rep.aspx' ", conn);
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
            OutputLabel.Visible = true;
            HyperLink1.Visible = true;
            Label path = (Label)FormView1.FindControl("itmlstLabel");
            HyperLink1.Text = path.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + path.Text;

            if (path.Text == "")
            {
                Label1.Text = "No Csv Exists";
                Response.Write("<script>window.location.href='trans_order.aspx'</script>");
            }
        }
        catch
        {

        }
    }
}
