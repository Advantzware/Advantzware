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

public partial class ageinvrep: System.Web.UI.Page
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
            string vPage = "ageinvrep.aspx";
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
                Image4.Visible = false;
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
            HyperLink1.Visible = false;
            OutPutFile.Visible = false;
            ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = Convert.ToString(Session["Customers_Company"]);



            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'ageinvrep.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    TextBoxdate.Text = dr["field1"].ToString();
                    BeSmanTextBox.Text = dr["field2"].ToString();
                    EndSmanTextBox.Text = dr["field3"].ToString();
                    BeCustTextBox.Text = dr["field4"].ToString();
                    EndCustTextBox.Text = dr["field5"].ToString();
                    BeItemTextBox.Text = dr["field6"].ToString();
                    EndItemTextBox.Text = dr["field7"].ToString();
                    BeJob1TextBox.Text = dr["field8"].ToString();

                    EndJob1TextBox.Text = dr["field9"].ToString();
                    BeJob2TextBox.Text = dr["field10"].ToString();
                    EndJob2TextBox.Text = dr["field11"].ToString();
                    BeWareTextBox.Text = dr["field12"].ToString();
                    EndWareTextBox.Text = dr["field13"].ToString();
                    ClassTextBox.Text = dr["field14"].ToString();
                    day1TextBox.Text = dr["field15"].ToString();
                    day2TextBox.Text = dr["field16"].ToString();
                    day3TextBox.Text = dr["field17"].ToString();
                    day4TextBox.Text = dr["field18"].ToString();

                    if (dr["chk_field1"].ToString() == "True")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;

                    if (dr["chk_field2"].ToString() == "True")
                        CheckBox2.Checked = true;
                    else
                        CheckBox2.Checked = false;

                    if (dr["chk_field3"].ToString() == "True")
                        CheckBox3.Checked = true;
                    else
                        CheckBox3.Checked = false;
                    if (dr["chk_field4"].ToString() == "True")
                        CheckBox4.Checked = true;
                    else
                        CheckBox4.Checked = false;

                    if (dr["chk_field5"].ToString() == "True")
                        CheckBox5.Checked = true;
                    else
                        CheckBox5.Checked = false;

                    if (dr["chk_field6"].ToString() == "True")
                        CheckBox6.Checked = true;
                    else
                        CheckBox6.Checked = false;
                    if (dr["chk_field7"].ToString() == "True")
                        CheckBox7.Checked = true;
                    else
                        CheckBox7.Checked = false;

                    if (dr["chk_field8"].ToString() == "True")
                        CheckBox8.Checked = true;
                    else
                        CheckBox8.Checked = false;


                    if (dr["rd_field2"].ToString() == "Quantity")
                        RadioButtonList_price.SelectedIndex = 0;
                    if (dr["rd_field2"].ToString() == "Value")
                        RadioButtonList_price.SelectedIndex = 1;


                    if (dr["rd_field1"].ToString() == "Avg")
                        RadioButtonList_show.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "Sell")
                        RadioButtonList_show.SelectedIndex = 1;

                    if (dr["rd_field3"].ToString() == "Comments")
                        RadioButtonList1.SelectedIndex = 0;
                    if (dr["rd_field3"].ToString() == "Days Old")
                        RadioButtonList1.SelectedIndex = 1;

                    if (dr["rd_field4"].ToString() == "Item#")
                        RadioButtonList2.SelectedIndex = 0;
                    if (dr["rd_field4"].ToString() == "Customer Part#")
                        RadioButtonList2.SelectedIndex = 1;

                    
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
                if (BeCustTextBox.Text == "")
                {
                    Label begin = (Label)FormView2.FindControl("CustLabel");
                    BeCustTextBox.Text = begin.Text;
                    EndCustTextBox.Text = begin.Text;
                }
                if (EndJob1TextBox.Text == "")
                    EndJob1TextBox.Text = "zzzzzz";
                if (BeJob2TextBox.Text == "")
                    BeJob2TextBox.Text = "-00";
                if (EndJob2TextBox.Text == "")
                    EndJob2TextBox.Text = "-99";
                if (EndItemTextBox.Text == "")
                    EndItemTextBox.Text = "zzzzzzzzzzzzzzz";
                if (EndWareTextBox.Text == "")
                    EndWareTextBox.Text = "zzzzz";
                if (EndSmanTextBox.Text == "")
                    EndSmanTextBox.Text = "zzz";
                if (EndCustTextBox.Text == "")
                    EndCustTextBox.Text = "zzzzzzzz";

                if (RadioButtonList1.SelectedValue == "")
                    RadioButtonList1.SelectedIndex = 0;
                if (RadioButtonList2.SelectedValue == "")
                    RadioButtonList2.SelectedIndex = 0;
                
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
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];




        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Aged";
        ObjectDataSource1.SelectParameters["prmAsof"].DefaultValue = TextBoxdate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeSman"].DefaultValue = BeSmanTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndSman"].DefaultValue = EndSmanTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeCust"].DefaultValue = BeCustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCust"].DefaultValue = EndCustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeItem"].DefaultValue = BeItemTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndItem"].DefaultValue = EndItemTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeJob"].DefaultValue = BeJob1TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndJob"].DefaultValue = EndJob1TextBox.Text.Trim(); ;
        ObjectDataSource1.SelectParameters["prmBeJob2"].DefaultValue = BeJob2TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndjob2"].DefaultValue = EndJob2TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeWare"].DefaultValue = BeWareTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndWare"].DefaultValue = EndWareTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmInvClass"].DefaultValue = ClassTextBox.Text.Trim();

        ObjectDataSource1.SelectParameters["prmAgedDay1"].DefaultValue = day1TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAgedDay2"].DefaultValue = day2TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAgedDay3"].DefaultValue = day3TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAgedDay4"].DefaultValue = day4TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRdPrice"].DefaultValue = Convert.ToString(RadioButtonList_price.SelectedValue);
        ObjectDataSource1.SelectParameters["prmQtyValue"].DefaultValue = Convert.ToString(RadioButtonList_show.SelectedValue);
        ObjectDataSource1.SelectParameters["prmComment"].DefaultValue = Convert.ToString(RadioButtonList1.SelectedValue);
        ObjectDataSource1.SelectParameters["prmSort"].DefaultValue = Convert.ToString(RadioButtonList2.SelectedValue);

        ObjectDataSource1.SelectParameters["prmCustWhse"].DefaultValue = Convert.ToString(CheckBox1.Checked);
        ObjectDataSource1.SelectParameters["prmPgBreak"].DefaultValue = Convert.ToString(CheckBox3.Checked);
        ObjectDataSource1.SelectParameters["prmTbCurr"].DefaultValue = Convert.ToString(CheckBox5.Checked);
        ObjectDataSource1.SelectParameters["prmCustPart"].DefaultValue = Convert.ToString(CheckBox7.Checked);
        ObjectDataSource1.SelectParameters["prmCost"].DefaultValue = Convert.ToString(CheckBox2.Checked);
        ObjectDataSource1.SelectParameters["prmNegSale"].DefaultValue = Convert.ToString(CheckBox4.Checked);
        ObjectDataSource1.SelectParameters["prmValCust"].DefaultValue = Convert.ToString(CheckBox6.Checked);
        ObjectDataSource1.SelectParameters["prmLastShip"].DefaultValue = Convert.ToString(CheckBox8.Checked);
        
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'ageinvrep.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8,field9,field10,field11, field12, field13, field14, field15, field16, field17, field18, chk_field1, chk_field2, chk_field3,chk_field4, chk_field5, chk_field6,chk_field7, chk_field8, rd_field1, rd_field2, rd_field3,rd_field4) values ('" + UserLogin.UserName + "','ageinvrep.aspx' , '" + TextBoxdate.Text.Trim() + "', '" + BeSmanTextBox.Text.Trim() + "','" + EndSmanTextBox.Text.Trim() + "','" + BeCustTextBox.Text.Trim() + "','" + EndCustTextBox.Text.Trim() + "','" + BeItemTextBox.Text.Trim() + "','" + EndItemTextBox.Text.Trim() + "','" + BeJob1TextBox.Text.Trim() + "','" + EndJob1TextBox.Text.Trim() + "','" + BeJob2TextBox.Text.Trim() + "','" + EndJob2TextBox.Text.Trim() + "','" + BeWareTextBox.Text.Trim() + "','" + EndWareTextBox.Text.Trim() + "','" + ClassTextBox.Text.Trim() + "','" + day1TextBox.Text.Trim() + "','" + day2TextBox.Text.Trim() + "','" + day3TextBox.Text.Trim() + "','" + day4TextBox.Text.Trim() + "','" + Convert.ToString(CheckBox1.Checked) + "','" + Convert.ToString(CheckBox2.Checked) + "','" + Convert.ToString(CheckBox3.Checked) + "','" + Convert.ToString(CheckBox4.Checked) + "','" + Convert.ToString(CheckBox5.Checked) + "','" + Convert.ToString(CheckBox6.Checked) + "','" + Convert.ToString(CheckBox7.Checked) + "','" + Convert.ToString(CheckBox8.Checked) + "','" + Convert.ToString(RadioButtonList_price.SelectedValue) + "','" + Convert.ToString(RadioButtonList_show.SelectedValue) + "','" + Convert.ToString(RadioButtonList1.SelectedValue) + "','" + Convert.ToString(RadioButtonList2.SelectedValue) + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + TextBoxdate.Text.Trim() + "', field2 = '" + BeSmanTextBox.Text.Trim() + "', field3 = '" + EndSmanTextBox.Text.Trim() + "', field4 = '" + BeCustTextBox.Text.Trim() + "', field5 = '" + EndCustTextBox.Text.Trim() + "', field6 = '" + BeItemTextBox.Text.Trim() + "', field7 = '" + EndItemTextBox.Text.Trim() + "', field8 = '" + BeJob1TextBox.Text.Trim() + "',field9 = '" + EndJob1TextBox.Text.Trim() + "', field10 = '" + BeJob2TextBox.Text.Trim() + "', field11 = '" + EndJob2TextBox.Text.Trim() + "', field12 = '" + BeWareTextBox.Text.Trim() + "', field13 = '" + EndWareTextBox.Text.Trim() + "', field14 = '" + ClassTextBox.Text.Trim() + "', field15 = '" + day1TextBox.Text.Trim() + "', field16 = '" + day2TextBox.Text.Trim() + "', field17 = '" + day3TextBox.Text.Trim() + "', field18 = '" + day4TextBox.Text.Trim() + "', chk_field1 = '" + Convert.ToString(CheckBox1.Checked) + "', chk_field2 = '" + Convert.ToString(CheckBox2.Checked) + "', chk_field3 = '" + Convert.ToString(CheckBox3.Checked) + "',chk_field4 = '" + Convert.ToString(CheckBox4.Checked) + "', chk_field5 = '" + Convert.ToString(CheckBox5.Checked) + "', chk_field6 = '" + Convert.ToString(CheckBox6.Checked) + "',chk_field7 = '" + Convert.ToString(CheckBox7.Checked) + "', chk_field8 = '" + Convert.ToString(CheckBox8.Checked) + "',  rd_field1 = '" + Convert.ToString(RadioButtonList_price.SelectedValue) + "', rd_field2 = '" + Convert.ToString(RadioButtonList_show.SelectedValue) + "', rd_field3= '" + Convert.ToString(RadioButtonList1.SelectedValue) + "',  rd_field4= '" + Convert.ToString(RadioButtonList2.SelectedValue) + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'ageinvrep.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("rageifileLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";                
            }
        }
        catch { }
        
    }
    
   
}
