
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

public partial class order_book_report_list : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        //ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "order_book_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
            if (aUsers == "external")
            {
                Image6.Visible = false;
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

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'order_book_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    TextBox1.Text = dr["field1"].ToString();
                    TextBox2.Text = dr["field2"].ToString();
                    bedateTextBox.Text = dr["field3"].ToString();
                    enddateTextBox.Text = dr["field4"].ToString();
                    besmanTextBox.Text = dr["field5"].ToString();
                    endsmanTextBox.Text = dr["field6"].ToString();
                    beproTextBox.Text = dr["field7"].ToString();
                    endproTextBox.Text = dr["field8"].ToString();                 

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
                    if (dr["chk_field6"].ToString() == "Yes")
                        CheckBox6.Checked = true;
                    else
                        CheckBox6.Checked = false;

                    if (dr["chk_field7"].ToString() == "Yes")
                        CheckBox7.Checked = true;
                    else
                        CheckBox7.Checked = false;

                    if (dr["chk_field8"].ToString() == "Yes")
                        CheckBox8.Checked = true;
                    else
                        CheckBox8.Checked = false;                    


                    if (dr["rd_field1"].ToString() == "Yes")
                        RadioButtonList1.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "NO")
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
                if (TextBox1.Text == "")
                {
                    Label begin = (Label)FormView2.FindControl("CustLabel");
                    TextBox1.Text = begin.Text;
                    TextBox2.Text = begin.Text;
                }
                if (TextBox2.Text == "")
                    TextBox2.Text = "zzzzzzzz";
                if (endsmanTextBox.Text == "")
                    endsmanTextBox.Text = "zzz";
                if (endproTextBox.Text == "")
                    endproTextBox.Text = "zzzzz";
                
                if (RadioButtonList1.SelectedValue == "")
                    RadioButtonList1.SelectedIndex = 0;

                RadioButtonList_out.SelectedIndex = 0;
            }
            catch { }

       
            if (Session["User"] != null)
            {
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


        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField1.Value = "Yes";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField1.Value = "NO";

        if (CheckBox1.Checked)
        {
            HiddenField2.Value = "Yes";
        }
        else
        {
            HiddenField2.Value = "No";
        }
        if (CheckBox2.Checked)
        {
            HiddenField3.Value = "Yes";
        }
        else
        {
            HiddenField3.Value = "No";
        } if (CheckBox3.Checked)
        {
            HiddenField4.Value = "Yes";
        }
        else
        {
            HiddenField4.Value = "No";
        } if (CheckBox4.Checked)
        {
            HiddenField5.Value = "Yes";
        }
        else
        {
            HiddenField5.Value = "No";
        } if (CheckBox5.Checked)
        {
            HiddenField6.Value = "Yes";
        }
        else
        {
            HiddenField6.Value = "No";
        } if (CheckBox6.Checked)
        {
            HiddenField7.Value = "Yes";
        }
        else
        {
            HiddenField7.Value = "No";
        } if (CheckBox7.Checked)
        {
            HiddenField8.Value = "Yes";
        }
        else
        {
            HiddenField8.Value = "No";
        }

        if (CheckBox8.Checked)
        {
            HiddenField9.Value = "Yes";
        }
        else
        {
            HiddenField9.Value = "No";
        }

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];        

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmBookAct"].DefaultValue = "orderbook";
        ObjectDataSource1.SelectParameters["prmBeginCust"].DefaultValue = TextBox1.Text.Trim();
        
        ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = TextBox2.Text.Trim(); ;
        ObjectDataSource1.SelectParameters["prmBegdate"].DefaultValue = bedateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndDate"].DefaultValue = enddateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegsman"].DefaultValue = besmanTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndsman"].DefaultValue = endsmanTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegPro"].DefaultValue = beproTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndPro"].DefaultValue = endproTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSquare"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmPsman"].DefaultValue = HiddenField2.Value;
        ObjectDataSource1.SelectParameters["prmSortOrd"].DefaultValue = HiddenField3.Value;
        ObjectDataSource1.SelectParameters["prmIdesc"].DefaultValue = HiddenField4.Value;
        ObjectDataSource1.SelectParameters["prmTon"].DefaultValue = HiddenField5.Value;
        ObjectDataSource1.SelectParameters["prmPro"].DefaultValue = HiddenField6.Value;
        ObjectDataSource1.SelectParameters["prmMis"].DefaultValue = HiddenField7.Value;
        ObjectDataSource1.SelectParameters["prmComm"].DefaultValue = HiddenField8.Value;
        ObjectDataSource1.SelectParameters["prmAvailMargin"].DefaultValue = HiddenField9.Value;
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'order_book_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8, chk_field1, chk_field2, chk_field3, chk_field4, chk_field5,chk_field6, chk_field7, chk_field8, rd_field1) values ('" + UserLogin.UserName + "','order_book_report.aspx' , '" + TextBox1.Text.Trim() + "', '" + TextBox2.Text.Trim() + "','" + bedateTextBox.Text.Trim() + "','" + enddateTextBox.Text.Trim() + "','" + besmanTextBox.Text.Trim() + "','" + endsmanTextBox.Text.Trim() + "','" + beproTextBox.Text.Trim() + "','" + endproTextBox.Text.Trim() + "','" + HiddenField2.Value + "','" + HiddenField3.Value + "','" + HiddenField4.Value + "','" + HiddenField5.Value + "','" + HiddenField6.Value + "','" + HiddenField7.Value + "','" + HiddenField8.Value + "','" + HiddenField9.Value + "','" + HiddenField1.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBox1.Text.Trim() + "', field2 = '" + TextBox2.Text.Trim() + "', field3 = '" + bedateTextBox.Text.Trim() + "', field4 = '" + enddateTextBox.Text.Trim() + "', field5 = '" + besmanTextBox.Text.Trim() + "', field6 = '" + endsmanTextBox.Text.Trim() + "', field7 = '" + beproTextBox.Text.Trim() + "', field8 = '" + endproTextBox.Text.Trim() + "', chk_field1 = '" + HiddenField2.Value + "', chk_field2 = '" + HiddenField3.Value + "', chk_field3 = '" + HiddenField4.Value + "', chk_field4 = '" + HiddenField5.Value + "', chk_field5 = '" + HiddenField6.Value + "', chk_field6 = '" + HiddenField7.Value + "', chk_field7 = '" + HiddenField8.Value + "', chk_field8 = '" + HiddenField9.Value + "', rd_field1 = '" + HiddenField1.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'order_book_report.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("datFileLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                Response.Write("<script>window.location.href='order_book_report.aspx';</script>");
            }
        }
        catch { }
    }
   

}

