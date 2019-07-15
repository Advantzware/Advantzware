
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

public partial class freight_report_list : System.Web.UI.Page
{

   
    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "freight_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
            if (aUsers == "external")
            {


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }


        if (!Page.IsPostBack)
        {
            OutPutFile.Visible = false;
            HyperLink1.Visible = false;
            ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);



            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'freight_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    bedateTextBox.Text = dr["field1"].ToString();
                    enddateTextBox.Text = dr["field2"].ToString();
                    TextBox1.Text = dr["field3"].ToString();
                    TextBox2.Text = dr["field4"].ToString();
                    BeJobTextBox.Text = dr["field5"].ToString();
                    BeJob2TextBox.Text = dr["field6"].ToString();
                    EndJobTextBox.Text = dr["field7"].ToString();
                    EndJob2TextBox.Text = dr["field8"].ToString();
                    
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
                if (TextBox1.Text == "")
                {
                    Label begin = (Label)FormView2.FindControl("CustLabel");
                    TextBox1.Text = begin.Text;
                    TextBox2.Text = begin.Text;
                }
            }
            catch { }
            if(TextBox2.Text == "")
            TextBox2.Text = "zzzzzzzz";
            if (EndJobTextBox.Text == "")
            EndJobTextBox.Text = "zzzzzz";
            if (bedateTextBox.Text == "")
            bedateTextBox.Text = "01/01/2001";
            if (enddateTextBox.Text == "")
            enddateTextBox.Text = "12/31/2099";
            if (BeJob2TextBox.Text == "")
            BeJob2TextBox.Text = "-00";
            if (EndJob2TextBox.Text == "")
            EndJob2TextBox.Text = "-99";
            RadioButtonList_out.SelectedIndex = 0;
            if (Session["User"] != null)
            {
                //UserClass UserLogin = (UserClass)Session["User"]; 
                lblUser.Text = UserLogin.UserName;

            }

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

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmFreight"].DefaultValue = "FreightRep";
        ObjectDataSource1.SelectParameters["prmBeInvDate"].DefaultValue = bedateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndInvDate"].DefaultValue = enddateTextBox.Text.Trim(); ;
        ObjectDataSource1.SelectParameters["prmBeCust"].DefaultValue = TextBox1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeJob1"].DefaultValue = BeJobTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndJob1"].DefaultValue = EndJobTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeJob2"].DefaultValue = BeJob2TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndJob2"].DefaultValue = EndJob2TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'freight_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8) values ('" + UserLogin.UserName + "','freight_report.aspx' , '" + bedateTextBox.Text.Trim() + "', '" + enddateTextBox.Text.Trim() + "','" + TextBox1.Text.Trim() + "','" + TextBox2.Text.Trim() + "','" + BeJobTextBox.Text.Trim() + "','" + BeJob2TextBox.Text.Trim() + "','" + EndJobTextBox.Text.Trim() + "','" + EndJob2TextBox.Text.Trim() + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + bedateTextBox.Text.Trim() + "', field2 = '" + enddateTextBox.Text.Trim() + "', field3 = '" + TextBox1.Text.Trim() + "', field4 = '" + TextBox2.Text.Trim() + "', field5 = '" + BeJobTextBox.Text.Trim() + "', field6 = '" + BeJob2TextBox.Text.Trim() + "', field7 = '" + EndJobTextBox.Text.Trim() + "', field8 = '" + EndJob2TextBox.Text.Trim() + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'freight_report.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("vFrightLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                Response.Write("<script>window.location.href='freight_report.aspx'</script>");
            }
        }
        catch { }

    }
    }

